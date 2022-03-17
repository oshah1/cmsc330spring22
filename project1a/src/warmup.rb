def fib(n)
    arr = []
    
    if n ==1
	arr << 0
    else

        if n >= 2 then
	    arr << 0
	    arr << 1
	    #loop index b/c i can't be bothered to figure out recursion
	    i = 2
	    while i < n do
	      num1 = arr[i-1]
	      num2 = arr[i-2]
	      nFibNum = num1 + num2
	      arr << nFibNum
	      i=i+1
	    end
        end
    end

    return arr
end

def isPalindrome(n)
    #convert n to a string
    str = n.to_s
    puts n
    #reverse the string
    rev = ""
    #append each char of n to rev
    i = str.length
  

  while i>0 do
	rev = rev + str[i-1]
    	i = i-1
  end
    return str.eql?(rev)
  
end

def nthmax(n, a)
    if n >= a.length then
	return nil
    end

    nMax=0

    #sort array
    sorted = a.sort.reverse

    #return number at position n-1
    return sorted[n]
end

def freq(s)
    if s.length < 1 then
	return s
    end

    #create hash to store each char in s along with the frequency of each char
    chars = Hash.new()

    #iterate through string s
    for i in 0..(s.length-1) do
    	#if chars has the given character as a key, increment the
	#value stored at theat key by 1. otherwise, create a new entry with 1 as an initial value
	
	
	if chars.has_key?(s[i]) then
	    chars[s[i]] += 1
	else #otherwise append the character to end of chars and increment the frequency by 1
	    chars[s[i]] = 1
	    
	end
    end

    #next find the character with the highest frequency
    max = 0
    max_char = ''
    chars.each {|key,value| if value > max then max = value; max_char = key end}
    return max_char
end


def zipHash(arr1, arr2)
    hash = {}
    arr_length = arr1.length
    #check which array is longer
    if arr1.length != arr2.length then
	return nil
    else
	i = 0
	while i < arr_length do
	    hash[arr1[i]] = arr2[i]
	    i = i+1
	end
    end

    return hash
end

def hashToArray(hash)
    #create 2D array
    arr = Array.new
    #iterate through hash
    #each key-value pair is an array that will by pushed onto arr
    hash.each { |key, value| arr.push([key, value]) }
    return arr
end
