private
removeFrom: start to: end
	"remove all the characters in the given range. 
	Note that start is 0 based as it is the start point of the selection, left from the character"
	characters := characters copyReplaceFrom: (start + 1) to: end with: ''.