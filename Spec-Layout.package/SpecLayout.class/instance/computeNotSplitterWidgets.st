private
computeNotSplitterWidgets

	(self commands reject: [ :e | e isSplitter ]) 
		do: [ :e | self computeNotSplitterWidget: e ]