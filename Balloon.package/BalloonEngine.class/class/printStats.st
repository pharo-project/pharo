accessing
printStats
	"BalloonEngine doProfileStats: true"
	"BalloonEngine printStats"
	"BalloonEngine resetStats"
	Transcript cr; nextPutAll:'/************** BalloonEngine statistics ****************/'.
	self printStat: (Times at: 1) count: (Counts at: 1) string: 'Initialization'.
	self printStat: (Times at: 2) count: (Counts at: 2) string: 'Finish test'.
	self printStat: (Times at: 3) count: (Counts at: 3) string: 'Fetching/Adding GET entries'.
	self printStat: (Times at: 4) count: (Counts at: 4) string: 'Adding AET entries'.
	self printStat: (Times at: 5) count: (Counts at: 5) string: 'Fetching/Computing fills'.
	self printStat: (Times at: 6) count: (Counts at: 6) string: 'Merging fills'.
	self printStat: (Times at: 7) count: (Counts at: 7) string: 'Displaying span buffer'.
	self printStat: (Times at: 8) count: (Counts at: 8) string: 'Fetching/Updating AET entries'.
	self printStat: (Times at: 9) count: (Counts at: 9) string: 'Changing AET entries'.
	Transcript cr; print: Times sum; nextPutAll:' mSecs for all operations'.
	Transcript cr; print: Counts sum; nextPutAll: ' overall operations'.
	Transcript endEntry.