class Array

  MUTATE = true
  INFINITY = 1 / 0.0
  
  @@falsey = proc do 
    false
  end
  
  @@identity = proc do |v|
    v
  end
  
  @@get_element = proc do |index|
    proc do |array|
      array[index]
    end
  end	
  
  @@base_chunk = lambda do |array, size|
    start_index, end_index, length, result = 0, size, array.size, []
    while start_index < length 
      result.push(array.slice(start_index...end_index))
      start_index, end_index = end_index, end_index + size
    end
    return result
  end
  
  @@base_difference = lambda do |array, arrays, &iteratee|
    arrays = self.map(self.flatten(arrays), &iteratee)
    return self.filter(array) { |e| !arrays.include?(iteratee.call(e)) }
  end
  
  @@base_uniq = lambda do |array, &iteratee| 
    hash = {}
    array.each do |e| 
      key = iteratee.call(e)
      if !hash.has_key?(key)
        hash[key] = e
      end
    end
    return hash.values
  end
  
  @@base_xor = lambda do |arrays, &iteratee| 
    result = []
    arrays.each_with_index do |current, i|
      arrays.each_with_index do |e, j|
	if i == j 
	  next
	end
	current = self.difference(current, e, &iteratee)
      end
      result += current
    end
    return result
  end
  
  @@base_reverse = lambda do |array| 
    i, j = 0, array.size - 1
    while i < j 
      array[i], array[j] = array[j], array[i]
      i += 1
      j -= 1
    end
    return array
  end
  
  @@base_rotate = lambda do |array, count|
    while count != 0
      if count > 0
        array.push(array.shift)
	count -= 1
      else 
	array.unshift(array.pop)
	count += 1
      end
    end
    return array
  end
  
  @@base_filter = lambda do |array, mutate=false, &predicate|
    result = []
    array.each do |e|
      if predicate.call(e)
        result.push(e)
      end
    end
    if mutate 
      array[0...array.size] = array - result
    end 
    return result
  end
  
  @@base_flatten_depth = lambda do |array, result, depth|
    array.each do |e|
      if e.instance_of?(Array) && depth > 0
        @@base_flatten_depth.call(e, result, depth - 1)
      else 
	result.push(e)
      end
    end
    return result
  end
  
  @@base_zip = lambda do |arrays, &iteratee| 
    length, result = (self.map(arrays) { |e| e.size }).min, []
    (length || 0).times do |i| 
      result.push(self.map(arrays, &@@get_element.call(i)))
    end
    return self.map(result, &iteratee)
  end
  
  @@tree = lambda do |array, children, path| 
    array.each_with_index do |e, i| 
      if path.include?(i)
        next 
      end
      path.push(i)
      node, node[:value], node[:children] = {}, e, []
      children.push(node)
      @@tree.call(array, node[:children], path)
      path.pop
    end
    return children
  end
  
  @@walk = lambda do |result, children, chunk, prefix| 
    if children.size == 0
      result.push(chunk)
    end
    children.each do |e| 
      prefix.push(e[:value])
      chunk.push(e[:value])
      @@walk.call(result, e[:children], chunk, prefix)
      prefix.pop
      chunk = prefix.slice(0...prefix.size)
    end
    return result
  end

  @@base_permutation = lambda do |array| 
    return @@walk.call([], @@tree.call(array, [], []), [], [])
  end

  @@predicate = lambda do |array, is_every=false, &predicate| 
    if !array.instance_of?(Array) || array.size == 0
      return is_every ? true : false
    end
    if !predicate
      predicate = @@identity
    end
    array.each_with_index do |e, i| 
      if is_every ? !predicate.call(e, i, array) : predicate.call(e, i, array)
        return is_every ? false : true
      end
    end
    return is_every ? true : false
  end
  
  @@slice = lambda do |array, n, is_drop=false, is_right=false|
    if !array.instance_of?(Array) || array.size == 0
      return []
    end
    n, length = (n.to_i rescue n == INFINITY ? array.size : 0), array.size
    if is_right 
      n = length - n 
    end
    n = [n, 0].max
    return is_drop ? (is_right ? array.slice(0...n) : (array.slice(n...length) || [])) : (is_right ? (array.slice(n...length) || []) : array.slice(0...n))
  end

  @@slice_while = lambda do |array, is_drop=false, is_right=false, &predicate| 
    if !array.instance_of?(Array) || array.empty?
      return []
    end
    if !predicate
      predicate = @@identity
    end
    index, length = is_right ? array.size - 1 : 0, array.size
    while is_right ? index > -1 : index < length
      if !predicate.call(array[index], index, array)
        break
      end
      index += (is_right ? -1 : 1)
    end
    return is_drop ? (is_right ? (index > -1 ? array.slice(0..index) : []) : array.slice(index...length)) : (is_right ? array.slice((index + 1)...length) : array.slice(0...index))
  end	
  
  @@base_reduce = lambda do |*args, is_right, &iteratee| 
    array, accumulator = args
    if args.size > 2
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 2)"
    end
    if !array.instance_of?(Array) || array.empty?
      return nil
    end
    if !iteratee
      iteratee = @@identity
    end
    is_init_accum = args.size == 2
    (is_right ? array.size - 1 : 0).step(is_right ? 0 : array.size - 1, is_right ? -1 : 1) do |i| 
      if !is_init_accum 
	is_init_accum, accumulator = true, array[i]
      else 
	accumulator = iteratee.call(accumulator, array[i], i, array)
      end
    end
    return accumulator
  end
  
  @@base_group_by = lambda do |array, &iteratee| 
    result = {}
    array.each do |e| 
      key = iteratee.call(e)
      if result.has_key?(key)
	result[key].push(e)
      else 
	result[key] = [e]
      end
    end
    return result
  end
  
  @@base_find = lambda do |array, from, is_right=false, &predicate| 
    if !array.instance_of?(Array) || array.empty?
      return nil
    end
    from = from.to_i rescue from == INFINITY ? array.size : 0
    if from < 0
      from = [0, from + array.size].max
    elsif is_right
      from = [array.size - 1, from].min
    end	  
    if !predicate
      predicate = @@identity
    end
    from.step(is_right ? 0 : array.size - 1, is_right ? -1 : 1) do |i| 
      if predicate.call(array[i], i, array)
	return array[i]
      end
    end
    return nil
  end

  def self.filter(array=[], &predicate)
    if !array.instance_of?(Array) || array.size == 0
      return []
    end
    if !block_given?
      predicate = @@falsey
    end
    return @@base_filter.call(array, &predicate)
  end
  
  def self.map(array=[], &iteratee)
    if !array.instance_of?(Array) || array.size == 0
      return []
    end
    if !block_given?
      iteratee = @@identity
    end
    return array.map(&iteratee)
  end
  
  def self.reduce(*args, &iteratee)
    return @@base_reduce.call(*args, false, &iteratee)
  end
  
  def self.reduce_right(*args, &iteratee)
    return @@base_reduce.call(*args, true, &iteratee)
  end

  def self.chunk(array=[], size=1)
    if !array.instance_of?(Array) || array.size == 0
      return []
    end 
    size = [(size.to_i rescue (size == INFINITY ? array.size : 0)), 0].max
    if size == 0
      return []
    end 
    return @@base_chunk.call(array, size)
  end
  
  def self.compact(array=[], mutate=false)
    if !array.instance_of?(Array) || array.size == 0
      return mutate ? array : []
    end 
    result = self.filter(array) { |e| e && e != 0 && e != '' && e != [] && e != {} && e == e }
    if mutate 
      array[0...array.size] = result
      return array
    end
    return result
  end
  
  def self.concat(array=[], *values)
    return !array.instance_of?(Array) ? [] : self.flatten([array] + values) 
  end
  
  def self.difference(array=[], *values, &iteratee)
    if !array.instance_of?(Array) || array.size == 0
      return []
    end
    if !block_given?
      iteratee = @@identity
    end
    arrays = self.filter(values) { |e| e.instance_of?(Array) }
    return @@base_difference.call(array, arrays, &iteratee)
  end
  
  def self.uniq(array=[], &iteratee)
    if !array.instance_of?(Array) || array.size == 0
      return []
    end
    if !block_given?
      iteratee = @@identity
    end
    return @@base_uniq.call(array, &iteratee)
  end
  
  def self.drop(array=[], n=1)
    return @@slice.call(array, n, true)
  end
  
  def self.drop_right(array=[], n=1)
    return @@slice.call(array, n, true, true)
  end
  
  def self.drop_while(array=[], &predicate)
    return @@slice_while.call(array, true, &predicate)
  end
  
  def self.drop_right_while(array=[], &predicate)
    return @@slice_while.call(array, true, true, &predicate)
  end
  
  def self.take(array=[], n=1)
    return @@slice.call(array, n)
  end
  
  def self.take_right(array=[], n=1)
    return @@slice.call(array, n, false, true)
  end
  
  def self.take_while(array=[], &predicate)
    return @@slice_while.call(array, &predicate)
  end
  
  def self.take_right_while(array=[], &predicate)
    return @@slice_while.call(array, false, true, &predicate)
  end
  
  def self.xor(*arrays, &iteratee)
    arrays = self.map(self.filter(arrays) {|e| e.instance_of?(Array) }) { |e| Array.uniq(e) }
    if !block_given?
      iteratee = @@identity
    end
    return @@base_xor.call(arrays, &iteratee)
  end
  
  def self.intersection(*arrays, &iteratee)
    arrays = self.map(self.filter(arrays) { |e| e.instance_of?(Array) }) { |e| Array.uniq(e) }
    if arrays.empty?
      return []
    end
    array, other = arrays.first, self.drop(arrays)
    if !block_given?
      iteratee = @@identity
    end
    return self.filter(array) { |e| Array.every(other) { |array| Array.map(array, &iteratee).include?(iteratee.call(e)) } }	
  end
  
  def self.union(*arrays, &iteratee)
    arrays = self.flatten(self.filter(arrays) { |e| e.instance_of?(Array) })
    if arrays.empty?
      return []
    end
    if !block_given?
      iteratee = @@identity
    end
    return self.uniq(arrays, &iteratee)
  end
	
  def self.flatten_depth(array=[], depth=1, mutate=false)
    if !array.instance_of?(Array) || array.size == 0
      return mutate ? array : []
    end
    depth = [(depth.to_i rescue (depth == INFINITY ? depth : 0)), 0].max
    result = @@base_flatten_depth.call(array, [], depth)
    if mutate 
      array[0...array.size] = result
      return array
    end
    return result
  end
  
  def self.flatten(array=[], mutate=false)
    return self.flatten_depth(array, 1, mutate)
  end
  
  def self.flatten_deep(array=[], mutate=false)
    return self.flatten_depth(array, INFINITY, mutate)
  end
  
  def self.remove(array=[], &predicate)
    if !array.instance_of?(Array) || array.size == 0
      return []
    end
    if !block_given?
      predicate = @@falsey
    end
    return @@base_filter.call(array, MUTATE, &predicate)
  end
  
  def self.reverse(array=[])
    if !array.instance_of?(Array) || array.size == 0
      return array
    end
    return @@base_reverse.call(array)
  end
  
  def self.rotate(array=[], count=0)
    if !array.instance_of?(Array) || array.size == 0
      return array
    end
    count = count.to_i rescue 0
    return @@base_rotate.call(array, count)
  end
  
  def self.zip(*arrays, &iteratee)
    arrays = self.filter(arrays) { |e| e.instance_of?(Array) }
    if !block_given?
      iteratee = @@identity
    end
    return @@base_zip.call(arrays, &iteratee)
  end
  
  def self.unzip(arrays=[], &iteratee)
    if !arrays.instance_of?(Array) || arrays.size == 0
      return []
    end
    if !block_given?
      iteratee = @@identity
    end
    return self.zip(*arrays, &iteratee)
  end
  
  def self.zip_object(props=[], values=[])
    result = {}
    if !props.instance_of?(Array) || !values.instance_of?(Array) || props.empty?
      return result
    end
    props.each_with_index do |e, i| 
      result[e] = values[i]
    end
    return result
  end
  
  def self.combination(array=[], n=array.size, &iteratee)
    result, chunk, index = [], self.permutation(array, n).push([]), []
    0.upto(chunk.size - 2) do |i| 
      if index.include?(i)
        next
      end
      result.push(chunk[i])
      (i + 1).upto(chunk.size - 1) do |j| 
        if (chunk[i] - chunk[j]).size == 0
          index.push(j)
        end 
      end
    end
    return block_given? ? result.each(&iteratee) : result
  end

  def self.permutation(array=[], n=array.size, &iteratee)
    if !array.instance_of?(Array) || array.size == 0
      return []
    end
    n = n.to_i rescue (n == INFINITY ? array.size : 0)
    if n < 0 
      return []
    end
    result = self.uniq(self.map(@@base_permutation.call(array)) { |e| e.slice(0...n) })
    return block_given? ? result.each(&iteratee) : result	
  end

  def self.every(array=[], &predicate)
    return @@predicate.call(array, true, &predicate)
  end
  
  def self.some(array=[], &predicate)
    return @@predicate.call(array, &predicate)
  end
  
  def self.flat_map(array=[], &iteratee)
    if !block_given?
      iteratee = @@identity
    end
    return self.flatten(self.map(array, &iteratee))
  end
  
  def self.flat_map_deep(array=[], &iteratee)
    if !block_given?
      iteratee = @@identity
    end
    return self.flatten_deep(self.map(array, &iteratee))
  end
  
  def self.flat_map_depth(array=[], depth=1, &iteratee)
    if !block_given?
      iteratee = @@identity
    end
    return self.flatten_depth(self.map(array, &iteratee), depth)
  end
  
  def self.find(array=[], from=0, &predicate)
    return @@base_find.call(array, from, &predicate)
  end

  def self.find_last(array=[], from=array.size - 1, &predicate)
    return @@base_find.call(array, from, true, &predicate)
  end
  
  def self.group_by(array=[], &iteratee)
    if !array.instance_of?(Array) || array.empty?
      return {}
    end
    if !block_given?
      iteratee = @@identity
    end
    return @@base_group_by.call(array, &iteratee)
  end
  
  def self.key_by(array=[], &iteratee)
    if !array.instance_of?(Array) || array.empty?
      return {}
    end
    if !block_given?
      iteratee = @@identity
    end
    result = {}
    array.each do |e| 
      result[iteratee.call(e)] = e
    end
    return result
  end
  
  def self.partition(array=[], &predicate)
    result = [[], []]
    if !array.instance_of?(Array) || array.empty?
      return result
    end
    if !block_given?
      predicate = @@identity
    end
    array.each do |e| 
      result[predicate.call(e) ? 0 : 1].push(e)
    end
    return result
  end
end  
