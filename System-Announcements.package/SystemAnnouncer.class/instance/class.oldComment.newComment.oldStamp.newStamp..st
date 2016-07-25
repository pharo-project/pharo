triggering
class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp
	"A class was commented in the system."

	self announce: (ClassCommented class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp)