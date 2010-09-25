# These examples are taken from
# http://jashkenas.github.com/coffee-script/

song = ["do", "re", "mi", "fa", "so"]

ages = {
  max: 10
  ida: 9
  tim: 11
}

matrix = [
  1, 0, 1
  0, 0, 1
  1, 1, 0
]

eldest = if 24 > 21 then "Liz" else "Ike"

six = (one = 1) + (two = 2) + (three = 3)

My.mood = greatly_improved if true

# Unfancy JavaScript
if happy and knows_it
  cha_cha_cha()
  false

Account = (customer, cart) ->
  @customer: customer
  @cart: cart

  $('.shopping_cart').bind 'click', (event) =>
    @customer.purchase @cart

class Animal
  move: (meters) ->
    alert @name + " moved " + meters + "m."

  randomify: ->
    @name.replace(/^[\w_-]*$/g, "-")

class Snake extends Animal
  constructor: (name) ->
    @name: name

  move: ->
    alert "Slithering..."
    super 5

class Horse extends Animal
  constructor: (name) ->
    @name: name

  move: ->
    alert "Galloping..."
    super 45

sam = new Snake "Sammy the Python"
tom = new Horse "Tommy the Palomino"

sam.move()
tom.move()
if car.speed < speed_limit then accelerate()

print "My name is " + @name

gold = silver = the_field = "unknown"

award_medals = (first, second, rest...) ->
  gold:       first
  silver:     second
  the_field:  rest

contenders = [
  "Michael Phelps"
  "Liu Xiang"
]

award_medals contenders...

alert "Gold: " + gold
alert "Silver: " + silver
alert "The Field: " + the_field

# Eat lunch.
# what up
# love it.
lunch = eat food for food in ['toast', 'cheese', 'wine']

$('#demo').click ->
  asd
# sup
  # asd
  # asdasd
blah = true

okay


# Naive collision detection.
for roid in asteroids
  for roid2 in asteroids when roid isnt roid2
    roid.explode() if roid.overlaps roid2

years_old = max: 10, ida: 9, tim: 11

ages = for child, age of years_old
  child + " is " + age

grade = (student) ->
  if student.excellent_work
    "A+"
  else if student.okay_stuff
    if student.tried_hard then "B" else "B-"
  else
    "C"

