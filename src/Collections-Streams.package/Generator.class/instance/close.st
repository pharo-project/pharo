open/close
close
	"Close the receiving generator and unwind its ensure-blocks."

	continue ifNotNil:[continue unwindTo: home].
	continue := block := next := nil