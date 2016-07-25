hack
hasBindingThatBeginsWith: aString
	"this method should not be defined here, normally when the interaction model of the RubScrolledTextModel is set to nil the hasBindingThatBeginsWith: message is not sent. Check method RubScrolledTextModel hasBindingThatBeginsWith:. Now if we remove this method, we get an error with RubScrolledTextModel  which has a LessonView as interaction model. I have no idea where it is set."

	^ false