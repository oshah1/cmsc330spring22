class WaitingTime
    require "set"

    
    def initialize(filename)
      @wait_time = Hash.new(0)
      File.foreach(filename) do |line|
	
        if line =~ /^([A-Z][a-z]+), ([A-Z][a-z]+), (\d+):(\d\d)$/ 
	    if @wait_time[$2 + " " + $1] == nil
	     @wait_time[$2 + " " + $1] = 0
	    end
            @wait_time[$2 + " " + $1] += ($3.to_i * 60) + $4
	end
      end	       
    end
    
    def student_waited_for(student_name)
      if @wait_time[student_name] != nil then @wait_time[student_name]
      else 0
    end
    
    def total_wait_time()
      sum = 0
      @wait_times.values.each do |v|
	sum +=v
      end
      return sum 
    end
end

class DuckSorter
    def initialize(filename)
      @ducks = Hash.new([])
      IO.foreach(filename) { |line|
	if line =~ /^name: ([A-Z][a-z]+), attributes: ([a-z]+(, [a-z]+)*)/
         @ducks[$1] += [$2.split(", ")
	 @ducks[$1].uniq!
	end
      }
    end

    def get_attribute(name)
      @ducks[name] 
       
    end

    def search(attribute)
      arr = []
      @ducks.each{ |k,v|
	if v.include? (attribute)
	  arr.push(k)
	end
      } 
      return arr
    end
end
