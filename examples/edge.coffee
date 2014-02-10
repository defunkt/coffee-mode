# Edge cases

if string.match /\// or string.match /\x1b/ or string.match /a\/b/
  console.log "matched"

string = "Something with a \"double quote"
console.log string

string = 'Something with a \'single quote'
console.log string

heredoc = """
  Heredoc with a " double quote
"""
console.log heredoc

###
foo
bar
###
console.log "foo bar is commented with block comment"

### this is
block comment ###
console.log "after block comment"

### this is
block comment ####
console.log "after block comment with 4 hash marks"
