require 'webrick'

root =   "#{File.dirname(__FILE__)}/www";
puts root;
server = WEBrick::HTTPServer.new :Port => 8001, :DocumentRoot => root

trap 'INT' do server.shutdown end

server.start
