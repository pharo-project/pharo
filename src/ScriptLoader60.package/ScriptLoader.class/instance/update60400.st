pharo - updates
update60400
	"self new update60400"
	self withUpdateLog: '19723 Nuke SequenceableCollection>>#checkedAt:
	https://pharo.fogbugz.com/f/cases/19723

19722 Move setMultipleSelection to FTTableMorph
	https://pharo.fogbugz.com/f/cases/19722'.
	self loadTogether: self script60400 merge: false.
	self flushCaches.
