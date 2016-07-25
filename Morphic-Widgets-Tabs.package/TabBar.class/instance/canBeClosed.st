protocol
canBeClosed

	^ tabs allSatisfy: [ :tab | tab closeable ]