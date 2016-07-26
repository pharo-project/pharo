accessing
interestedInClass: aClass

	"answer if the browser is showing content related to aClass"
	
	^ (aClass = self selectedClass) or: [ aClass users includes: self selectedClass  ]