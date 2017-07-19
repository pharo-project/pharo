block closure support
numLocalTempsForBlockAt: startpc in: method
	"**	250		11111010 	eeiiikkk		jjjjjjjj		Push Closure Num Copied iii (+ExtA//16*8) Num Args kkk (+ ExtA\\16*8) BlockSize jjjjjjjj (+ExtB*256). ee = num extensions"
	^ BlockLocalTempCounter
		tempCountForBlockAt: startpc - 3 - (((method at: startpc - 2) >> 6) * 2)
		in: method 