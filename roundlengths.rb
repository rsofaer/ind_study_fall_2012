#! /usr/bin/ruby
lengths = []
1.upto(100) do
  lengths << rand(1025)
end
lengths.sort!
puts lengths.inspect
iprime = 0
lengthsPrime = Array.new(lengths)
0.upto(lengths.length - 1) do |n|
  if(lengths[n] > 2*lengths[iprime])
    iprime = n
  end
  lengthsPrime[n] = lengths[iprime]
end
puts lengthsPrime.inspect

