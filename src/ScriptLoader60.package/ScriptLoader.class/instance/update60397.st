pharo - updates
update60397
	"self new update60397"
	self withUpdateLog: '19715 FastTableModel: Cant enable multiple selection
	https://pharo.fogbugz.com/f/cases/19715

19211 sometimes I can not open changesort: receiver of endswith is nil
	https://pharo.fogbugz.com/f/cases/19211

19718 Give Random an option to initialize its seed from /dev/random
	https://pharo.fogbugz.com/f/cases/19718'.
	self loadTogether: self script60397 merge: false.
	self flushCaches.
