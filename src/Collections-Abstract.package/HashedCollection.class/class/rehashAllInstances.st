initialization
rehashAllInstances
	"Do not use #allInstancesDo: because rehash may create new instances."
	
	self allInstances do: [ :each | each rehash ]