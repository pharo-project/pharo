*Tool-Base
browseAllSelect: aBlock 
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result. For example,  
	SystemNavigation new browseAllSelect: [:method | method numLiterals >  
	10]."

	^ self
		browseMessageList: (self allMethodsSelect: aBlock)
		name: 'selected messages'