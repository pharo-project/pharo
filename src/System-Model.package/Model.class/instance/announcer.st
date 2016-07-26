as yet unclassified
announcer
	"pay attention we name this instance variable announceur and not announcer because subclasses may already have an announcer instance variable. We will remove them and rename this one in the future."
	^ announceur ifNil: [ announceur := Announcer new ]