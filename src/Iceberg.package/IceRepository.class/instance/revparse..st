private
revparse: aString 
	"Right now this is necessary because Metacello provides a tag objectId instead of a commitId"
	^ self backend revparse: aString