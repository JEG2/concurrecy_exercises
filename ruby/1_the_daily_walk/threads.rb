NAMES = %w[Alice Bob]

def do_task(doer: , wait: , task: )
  Thread.new do
    puts "#{doer} started #{task}\n"
    seconds = rand(wait)
    sleep seconds
    puts "#{doer} spent #{seconds} seconds #{task}\n"
  end
end

def do_task_when_ready(wait_on: , task: )
  Thread.new do
    wait_on.each(&:join)
    puts "#{task}\n"
  end
end

puts "Let's go for a walk!\n"

NAMES.map { |name|
  do_task(doer: name, wait: 60..90, task: "getting ready")
}.each(&:join)

puts "Arming alarm.\n"

busy = NAMES.map { |name|
  do_task(doer: name, wait: 35..45, task: "putting on shoes")
}
_ = do_task_when_ready(wait_on: busy, task: "Exiting and locking the door.")

puts "Alarm is counting down.\n"
sleep 60
puts "Alarm is armed.\n"
