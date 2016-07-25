private
addPostloadHacks
	"Simply perform #addHacks again. We've now loaded classes that weren't
	there before and some of those also need extensions" 
	(Smalltalk at: #FLPlatform) current addHacks