#!/usr/bin/env ruby

# == Cephyr Ocaml Preprocessor ==
#  This is a preprocessor for the Cephyr C compiler. This is NOT the preprocessor for C;
# rather, it's a preprocessor for OCaml. So we can debug with ease of use.
# Sure, there's ocpp and several other preprocessors for OCaml but I want _my_ preprocessor
# for _my_ project!
# This preprocessor still has LOTS of features to be added.
# Currently, it just has the include (incfile) directive.
# So, I guess, WIP?
# -- Chubak (github.com/Chubek) || github.com/Chubek/Cephyr || let-over-lambda.com

$DIR_PREFIX = '@'
$STR_LDELIM = '"'
$STR_RDELIM = '"'

$INCDIR_NAME = 'incfile'

class LexicalScanner
  attr_accessor :infile, :slave_log

  class IncludeDirective
    attr_accessor :path, :is_silent, :lno

    def initialize(path, is_silent, lno)
      @path = path
      @is_silent = is_silent
      @lno = lno
    end
  end

  def initialize(infile)
    @infile = handle_infile infile
    @slave_log = {}
  end

  def handle_infile(infile)
    return STDIN unless infile
    raise "Input file does not exist: " + infile unless File.exist? infile
    return File.open infile
  end

  def lexically_scan
    lno = 0

    until @infile.eof?
      lno += 1
      ln = @infile.readline

      unless ln.start_with? $DIR_PREFIX
        @slave_log[lno] = [ln, :BYPASS]
        next
      end

      if mk_incpatt.match ln
        @slave_log[lno] = [IncludeDirective.new($2, ($1 == '-s'), ln), :INCLUDE]
        next
      end
      
      raise "Syntax error"
    end

    @infile.close unless @infile == STDIN
    @slave_log
  end

  def mk_incpatt
    return Regexp.new '^' + $DIR_PREFIX + $INCDIR_NAME + "\s+(-s|-ns)\s+" + $STR_LDELIM + '(.+)' + $STR_RDELIM + "\s*$"
  end
end

class SyntacticPreprocessor
  attr_accessor :slave_log, :out_lines

  def initialize(slave_log)
    @slave_log = slave_log
    @out_lines = []
  end

  def handle_incdir(directive)
    return "\n" if !(File.exist? directive.path) && directive.is_silent
    return File.read directive.path if (File.exist? directive.path)
    raise "File does not exist: " + directive.path
  end

  def syntactically_preprocess
    @slave_log.each do |lno, slave|
      constr, action = slave
      if action == :BYPASS
        @out_lines << constr
      elsif action == :INCLUDE
        @out_lines << (handle_incdir constr)
      end
    end

    @out_lines
  end
end

class OutfileWriter
  attr_reader :out_lines, :outfile

  def initialize(out_lines, outfile)
    @outfile = handle_outfile outfile
    @out_lines = out_lines
  end

  def handle_outfile(outfile)
    return STDOUT unless outfile
    return File.open outfile
  end

  def write_outfile
    @out_lines.each { |oln| write_line oln }
    @outfile.close unless @outfile == STDOUT
  end

  def write_line(oln)
    @outfile.write (oln + "\n") 
  end
end

def parse_arguments
  options = { :infile => nil, :outfile => nil }
  return options unless ARGV.length > 0
  argv_prime = ARGV.dup
  first_arg = argv_prime.shift
  second_arg = argv_prime.shift
  third_arg = argv_prime.shift

  if first_arg.start_with? '-o'
    raise "Insuffient amount of arguments" unless second_arg
    options[:outfile] = second_arg
    options[:infile] = third_arg if third_arg
  else
    options[:infile] = first_arg
  end

  STDERR.puts "Input file set to: " + (options[:infile] == nil ? "STDIN" : options[:infile])
  STDERR.puts "Output file set to: " + (options[:outfile] == nil ? "STDOUT" : options[:outfile])

  options
end

options = parse_arguments

scanner = LexicalScanner.new options[:infile]
slave_log = scanner.lexically_scan
parser = SyntacticPreprocessor.new slave_log
out_lines = parser.syntactically_preprocess
outfile_writer = OutfileWriter.new out_lines, options[:outfile]
outfile_writer.write_outfile

STDERR.puts "File preprocessed successfully"







