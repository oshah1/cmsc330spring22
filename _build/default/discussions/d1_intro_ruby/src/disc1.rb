# We will be implimenting a simple database table using Ruby data structures to store the data.
# The class Tuple represents an entry in a table.
# The class Table represents a collection of tuples.

class Tuple
    @hash = Hash.new(0)

    # data is an array of values for the tuple
    def initialize(data)
	@data = data
	@@hash[data.size] += 1
        
    end

    # This method returns the data at a particular index of a tuple (0 indexing)
    # If the provided index exceeds the largest index in the tuple, nil should be returned.
    # index is an Integer representing a valid index in the tuple.
    def getData(index)
	if index > (getSize() -1) then
	    return nil
	else
	return @data[index]
        end
    end

    def getSize()
	return @data.size
    end
    # This method should return the number of tuples of size n that have ever been created
    # hint: you should use a static variable
    # hint2: a hash can be helpful (though not strictly necessary!)
    def self.getNumTuples(n)
	 return @hash[n]
        
    end
end

class Table
    
    # column_names is an Array of Strings
    def initialize(column_names)
	@column_names = column_names;
	@tuples = [];
        
    end

    # This method inserts a tuple into the table.
    # Note that tuples inserted into the table must have the right number of entries
    # I.e., the tuple should be the size of column_names
    # If the tuple is the correct size, insert it and return true
    # otherwise, DO NOT insert the tuple and return false instead.
    # tuple is an instance of class Tuple declared above.
    def insertTuple(tuple)
        col_length = @column_names.length

	tuple_size = tuple.getSize;

        if tuple_size != col_length then
            false
        else
            @tuples.push(tuple);
            true
        end
    end
    
    # Given a column name and a value, this method finds the number of rows where the value 
    # for the column matches the given value.
    def numRowsWhere(column,value)
	count = 0

	column_index = @column_names.index(column);
	for tuple in @tuples do
	    
	    if tuple.getData(column_index) == value then
		count += 1
	    end

	end

	return count
    end 

end