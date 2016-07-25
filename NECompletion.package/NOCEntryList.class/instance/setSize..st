accessing
setSize: n
	size := n.
	entries := Dictionary new: n.
	entryHead := NOCDatedEntry contents: #'' size type: #selector.
	entryHead
		next: entryHead;
		previous: entryHead.
	
	