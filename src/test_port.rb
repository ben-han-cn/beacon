require 'json'

output = File.open("test.txt", "w")
while true
    begin 
        len_data = STDIN.read(2)
        byte_count = len_data.unpack("n")[0]
        output << "cmd_data_len: " << byte_count << "\n"
        line = STDIN.read(byte_count)
        cmd = JSON.load(line)
        output << "command_name: " << cmd["name"] << "\n"
        output << "command_args: " << cmd["args"] << "\n"
        output << "\n\n"
        STDOUT.write_nonblock(len_data)
        STDOUT.write_nonblock(line)
        break if line.include? "exit"
    rescue Exception => e
        output << "get exception : #{e}"
        break
    end
end
output.close

