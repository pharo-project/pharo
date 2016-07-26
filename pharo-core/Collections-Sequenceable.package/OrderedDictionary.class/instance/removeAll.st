removing
removeAll
	1 to: self size do: [:i | orderedKeys at: i put: nil].
	dictionary removeAll.