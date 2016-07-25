private
setMultipleContents
	| tabs size delta |

	contentsWrapper removeAllMorphs.
	self cleanProcesses.
	
	tabs := toolbar orderedSelectedTabs.
	size := tabs size.
	delta := 1 / size.
	
	tabs allButLast keysAndValuesDo: [ :index :tab | self addTabContents: tab at: index delta: delta ].
	self addLastTabContents: tabs last among: size delta: delta.
	self addSplitterOn: tabs delta: delta