testing
hasSpecialCTRLKeyValue
"
	4 - Character end
	1 - Character home
"

	^ self controlKeyPressed and: [ keyValue <= 26 & (keyValue ~= 4) & (keyValue ~= 1) ]