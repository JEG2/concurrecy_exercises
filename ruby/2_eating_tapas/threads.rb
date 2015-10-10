# coding: utf-8

class Dish
  NON_BLOCKING = true

  def initialize(name)
    @q = Queue.new
    rand(5..10).times do
      @q.push(name)
    end
  end

  attr_reader :q
  private     :q

  def next_morsel
    q.pop(NON_BLOCKING)
  rescue ThreadError
    nil
  end
end

class Person
  def initialize(name: , dishes: )
    @name   = name
    @dishes = dishes.dup
    @thread = nil
  end

  attr_reader :name, :dishes, :thread
  private     :name, :dishes, :thread

  def eat
    @thread = Thread.new do
      while (dish = dishes.sample)
        if (morsel = dish.next_morsel)
          puts "#{name} is enjoying some #{morsel}\n"
          sleep rand(30..(3 * 60))
        else
          dishes.delete(dish)
        end
      end
    end
  end

  def wait_on
    thread.join
  end
end

puts "Bon appétit!\n"

dishes = [
  "chorizo",
  "chopitos",
  "pimientos de padrón",
  "croquetas",
  "patatas bravas"
].map { |name| Dish.new(name) }
%w[Alice Bob Charlie Dave]
  .map { |name| Person.new(name: name, dishes: dishes) }
  .each(&:eat)
  .each(&:wait_on)

puts "That was delicious!\n"
