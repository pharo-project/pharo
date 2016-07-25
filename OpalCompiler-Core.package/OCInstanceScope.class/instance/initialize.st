initialization
initialize
  
	vars := Dictionary new.
	selfVar := OCSelfVariable new
		scope: self;
		yourself.
	superVar := OCSuperVariable new
		scope: self;
		yourself.