class PhoneBook
    
    def initialize
	#Hash that maps names to phone numbers
	@book = Hash.new
	#Another array that stores all the listed numbers
	@Listed_nums = Array.new
    end

    def add(name, number, is_listed)
	#First, check if the phone number is in the format "NNN-NNN-NNNN"
	format = number.split('-')
	re = /[0-9]{3}-[0-9]{3}=[0-9]{4}/
	if (format.length != 3) && (format[0].length != 3) && (format[1].length != 3) && (format[2] != 4) then
		return false
	end
	#Then, check if the person with this name already exists
	#Boolean flag called name_exists
	name_exists = false
	
	if @book.has_key?(name) then
	    return false
	end

	#if is_listed is true, add the number to Listed_nums
	if is_listed && @Listed_nums.include?(number) then
	    return false
	end

	@book[name] = number
	if is_listed then
	   @Listed_nums << number
	end

        return true
    end

    def lookup(name)
	if @book.has_key?(name) && @Listed_nums.include?(@book[name])
	    return @book[name]
	else
	    return nil
	end
    end

    def lookupByNum(number)
	output = ""
	#Check if the number is in the list listed_nums
	if !(@Listed_nums.include?(number)) then
	    return nil
	end
	@book.each {|key, value| if value == number then output = key end}
        return output
    end

    def namesByAc(areacode)
	names = Array.new
	#iterate over book
	#if the first three letters of the number match the area code, add the name to the array
	@book.each {|key, value| if value.slice(0..2) == areacode then names << key end}

        return names
    end
end
