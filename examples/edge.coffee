# Edge cases

if string.match /\// or string.match /\x1b/ or string.match /a\/b/
  console.log "matched"

string = "Something with a \"double quote"
console.log string

string = 'Something with a \'single quote'
console.log string

# TODO
heredoc = """
  Heredoc with a " double quote
"""
console.log heredoc
