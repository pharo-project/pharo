copying
, otherCollection 
	"Concatenate two Strings or Collections."
	
	^ self copyReplaceFrom: self size + 1
		  to: self size
		  with: otherCollection
"
#(2 4 6 8) , #(who do we appreciate)
((2989 storeStringBase: 16) copyFrom: 4 to: 6) , ' boy!'
"