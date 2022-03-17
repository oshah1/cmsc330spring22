class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
		@num_ships = 0
		@successful_attacks = 0
		#an hash table that uses each ship object as a key and an array of cells
		#that the ship occupies as the value
		@ships = Hash.new
		@attacks = Array.new
		#and a 3D array to store a representation of the board
		#y-axis: row, x-axis, col, z-axis: index 0: if a ship occupies that cell, ined 1: if an attack 
		#was made at that cell  
		@board = Array.new(max_row) {Array.new(max_column) {Array.new(2,false)}}
    end
	#note: position coordinates start at 1

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
	#check if the starting position is out of bounds
	ship_row = (ship.start_position.row)#y-coordinate of starting position
	ship_col = (ship.start_position.column)#x-coordinate of starting position
	size = ship.size#size of ship
	ship_cells = []
	if ship_row < 1 || ship_row > @max_row || ship_col < 1 || ship_col > @max_column then
		print "out of bounds"
	    return false
	end
	#check ship orientation
	if ship.orientation == "Up" then
		
	# if the ship is pointing up then we need to check if it's going to
	#cross the top of the board
	    if ship_row - size < 0 then
			print "ship too big"
			return false
	    end
	    #loop through the board, starting at the starting row of the current ship
	    
	    for i in (ship_row-size+1)..(ship_row) do
		#if cell at board[i-1][ship.position.col-1] has a ship, return false
			if @board[i-1][ship_col-1][0]==true then
				print "ship at \(#{i},#{ship_col}\)"
				return false
			end
			
	    end
	    
	    #edit the board
	    for i in (ship_row -size)..(ship_row-1) do
			@board[i-1][ship_col-1][0] = true
			#push an array 2/ two elements: the row of each cell and column of each cell
			ship_cells.push([i-1,ship_col-1,false])
	    end
	elsif ship.orientation == "Down" then
		
	    #if the ship is pointing down, check if it will overlap with the lower boundary of the map
	    if ship_row + size -1 > @max_row then
			print "ship too big"
			return false
	    else
			#iterate "downwards", stopping if a ship already occupies the cell we will place
			#the new ship
			for i in ship_row..(ship_row+size-1) do
				if @board[i-1][ship_col -1][0]==true then
					print "ship at \(#{i},#{ship_col}\)"
					return false
				end
			end
				
			#now edit the board
			for i in (ship_row)..(ship_row + size-1) do
				@board[i-1][ship_col-1][0] = true
				ship_cells.push([i-1,ship_col-1,false])
			end
	    end
	elsif ship.orientation=="Left" then
		
	    if ship_col - size < 0 then
			return false
			print "ship too big"
	    end
	    
	    #check if the new ship will overlap with a ship already on the board
	    #check the cells to the left of the starting position
	    for i in (ship_col - size+1)..(ship_col) do
			if @board[ship_row - 1][i-1][0]==true then
				print "ship at \(#{ship_row},#{i}\)"
				return false
			end
	    end

	    #Edit the board
	    for i in (ship_col - size)+1..(ship_col) do
			@board[ship_row- 1][i-1][0] = true
			ship_cells.push([ship_row-1,i-1,false])
	    end

	elsif ship.orientation=="Right" then
		
	    if ship_col + size-1 > @max_column then
			return false
	    end
		#iterate thru the hash of ships
		#A ship will intersect another if any part of the ship
		# falls between the start and end of any other ship so:
		
		
	    for i in (ship_col)..(ship_col+size-1) do
			if @board[ship_row-1][i-1][0] then
				return false
			end
	    end

	    #place the ship by changing the board
	    for i in (ship_col)..(ship_col + size-1) do
			@board[ship_row-1][i-1][0] = true
			ship_cells.push([ship_row-1,i-1,false])
	    end
	else
		return false
	end
		#add ship to @ships and increment num_ships
		@num_ships+=1
		puts ship.to_s
		@ships[ship]=ship_cells
		puts "#{@num_ships}"
		return true
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
	#note: add a flag to check if the attack was successful
	hit = false
	row = position.row
	col = position.column
        # check position
	if (row < 1) || (row > (@max_row)) || (col <1) || (col > (@max_column)) then
	    return nil
	end
	#check if there is a ship at the attack position
	if @board[row-1][col-1][0]==true then
		
		hit = true
		#check if the square has already been attacked
		if @board[row-1][col-1][1]==false then
	    	@successful_attacks+=1
		end
	end
    # update your grid
	@board[row-1][col-1][1] = true
	#check if any boats sank
	
    # return whether the attack was successful or not
	return hit
        
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @successful_attacks
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
		#keeps track of number of ship segments (cells occupied by a ship)
		ship_segs = 0
		#loop thru the board
		for i in 0..(@max_row-1) do
			for j in 0..(@max_column-1) do
				#if 1) there is a ship at this spot and 2) an attack landed at this spot
				if @board[i][j][0] == true then
					ship_segs+=1
				end
			end
		end
		#if the number of successfull attacks equals the number of segments hit, then
		#all ships were sunk
		puts "ship segments: #{ship_segs}"
        return ship_segs== @successful_attacks
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
		#first, print the numbers for the x-axis
		print '/n'
		for i in 1..@board.length do
			print "  " + i.to_s + "    "
		end
		#now, start printing each cell and the contents thereof
		
		#iterate through the rows of the board
		for i in 0..(@max_row -1) do
			print "\n#{i+1}:"
			for j in 0..(@max_column-1) do
				#if there is a ship in cell i,j print "B"
				if @board[i][j][0] then
					print " B"
				else
					print " -"
				end
				
				#if an attack was made at the cell, print "A"
				if @board[i][j][1] then
					print ", A |"
				else
					print ", - |"
				end
			end
		end
        
    end
end
