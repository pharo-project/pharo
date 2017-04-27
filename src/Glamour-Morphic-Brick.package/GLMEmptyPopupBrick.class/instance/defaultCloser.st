initialization
defaultCloser
	"Return a default popup closer responsible for hiding a popup"
	<return: #GLMPopupBrickCloser>

	^ GLMPopupBrickCloser new
		predicate: GLMPopupBrickUnhoverPredicate new;
		yourself