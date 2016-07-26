tests - accessing
testUpTo3
	| stream string |
	string := 'XYZabcdUVW'.
	stream := ReadStream on: string from: (string indexOf: $a) to: (string indexOf: $d).
	self assert: stream upToEnd = 'abcd'.
	self assert: stream atEnd.
	stream := ReadStream on: string from: (string indexOf: $a) to: (string indexOf: $d).
	self assert: (stream upTo: $c) = 'ab'.
	self assert: stream next = $d.
	stream := ReadStream on: string from: (string indexOf: $a) to: (string indexOf: $d).
	self assert: (stream upTo: $e) = 'abcd'.
	self assert: stream atEnd.