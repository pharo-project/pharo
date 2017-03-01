pharo - updates
update60420
	"self new update60420"
	self withUpdateLog: '19777 BaselineOfUI compiles own version of RubScrolledTextMorph>>#font:
	https://pharo.fogbugz.com/f/cases/19777

19772 BaselineOfIDE compiles own version of EpLostChangesDetector>>#startUp:
	https://pharo.fogbugz.com/f/cases/19772

18784 FileList selected directory entry is not cleared after selecting a new directory
	https://pharo.fogbugz.com/f/cases/18784

18871 DNU after showing about box of QA annotation question mark icon
	https://pharo.fogbugz.com/f/cases/18871

19774 BaselineOfUI is compiling own version of Pragma>>#methodSelector
	https://pharo.fogbugz.com/f/cases/19774'.
	self loadTogether: self script60420 merge: false.
	self flushCaches.
