# Testing imenu
regexp = /asdas/
two = 4 / 2

minus = (x, y) -> x - y

String::length = -> 10

class Person
  print: ->
    print 'My name is ' + this.name + '.'

app =
  window:  {width: 200, height: 200}
  para:    'Welcome.'
  button:  'OK'

block = ->
  print('potion')

Please = {}
Please.print = (word) ->
  print(word)

HomePage::get = (url) ->
  session: url.query.session if url.query?

class Policeman extends Person
  constructor: (rank) ->
    @rank: rank

  print: ->
    print 'My name is ' + this.name + " and I'm a " + this.rank + '.'
