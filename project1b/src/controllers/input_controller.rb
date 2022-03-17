require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    board = GameBoard.new 10, 10
    #variable to keep track of the number of valid ships added
    num_valid = 0
    if read_file_lines(path) {|line| 
        if line =~ /^\((\d+),(\d+)\), (\w+), (\d+)$/ then
        
            row = $1.to_i
            col = $2.to_i
            start_pos = Position.new(row,col); 
            new_ship = Ship.new(start_pos, $3, $4.to_i); 
            #check that the ship can be added
            if board.add_ship(new_ship) then
                num_valid +=1
            end
        end

        } && num_valid==5 then
        return board
    else
        return nil
    end
end



# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    result = []
    #regular expression that the proc uses
    re = /^\((\d+),(\d+)\)$/
    #Proc instance for read_file_lines to run
    
    if read_file_lines(path) {|line|
        if line =~ re then
            row = $1.to_i
            col = $2.to_i
            new_pos = Position.new(row,col)
            result.push(new_pos)
        end
    } then 
        return result
    else
        return nil
    end
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
