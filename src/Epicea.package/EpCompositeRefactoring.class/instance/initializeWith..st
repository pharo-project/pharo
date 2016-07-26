initializing
initializeWith: someRefactorings 
	self initialize.
	childrenRefactorings := someRefactorings collect: #asEpiceaEvent