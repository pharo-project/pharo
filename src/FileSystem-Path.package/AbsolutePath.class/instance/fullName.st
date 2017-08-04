accessing
fullName

	^ self delimiter asString, (self segments joinUsing: self delimiter)