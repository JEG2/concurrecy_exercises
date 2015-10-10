class Tourist
  def initialize(name: , computers: )
    @name      = name
    @computers = computers
    @thread    = nil
  end

  attr_reader :name, :computers, :thread
  private     :name, :computers, :thread

  def queue_up
    @thread = Thread.new do
      computer = wait_on_computer
      puts "#{name} is online.\n"
      time = rand(15..120)
      sleep time * 60
      puts "#{name} is done, having spent #{time} minutes online."
      computers.push(computer)
    end
  end

  def wait_on
    thread.join
  end

  private

  def wait_on_computer(non_block = true)
    computers.pop(non_block)
  rescue ThreadError
    puts "#{name} waiting for turn.\n"
    non_block = false
    retry
  end
end

computers = Queue.new
8.times do |i|
  computers.push("Computer #{i + 1}")
end

Array.new(25) { |i|
  Tourist.new(name: "Tourist #{i + 1}", computers: computers)
}.each(&:queue_up).each(&:wait_on)

puts "The place is empty, let's close up and go to the beach!\n"
