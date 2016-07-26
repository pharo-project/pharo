responses
dwbench: request
	"Reply with the dynamic DW-Bench HTML page"

	^ ZnResponse ok: (ZnEntity html: self generateDWBench)