copying
padLeftTo: length with: char
	^ (String new: (length - self size max: 0) withAll: char) , self.