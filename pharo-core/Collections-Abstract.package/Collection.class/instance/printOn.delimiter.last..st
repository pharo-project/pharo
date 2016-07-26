printing
printOn: aStream delimiter: delimString last: lastDelimString
	"Print elements on a stream separated
	with a delimiter between all the elements and with
	a special one before the last like: 'a, b and c'

	Note: Feel free to improve the code to detect the last element."

	| n sz |
	n := 1.
	sz := self size.
	self do: [:elem |
		n := n + 1.
		aStream print: elem]
	separatedBy: [
		n = sz
			ifTrue: [aStream print: lastDelimString]
			ifFalse: [aStream print: delimString]]