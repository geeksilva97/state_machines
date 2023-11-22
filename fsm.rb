TRANSITIONS = {
  initial: { header: :reading },
  reading: { data: :reading, trailer: :done }
}

state = :initial

while state != :done && state != :error do
  message_type = $stdin.gets.delete("\n")
  puts "Received message type -> #{message_type}"
  new_state = TRANSITIONS[state][message_type.to_sym] ||  :error
  puts "Transitioning from #{state} to #{new_state}"

  state = new_state
end
