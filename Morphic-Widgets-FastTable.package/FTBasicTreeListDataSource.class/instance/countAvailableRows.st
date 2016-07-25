private
countAvailableRows

	availableRows := rootItems inject: 0 into: [ :sum :item | sum + item availableRows ]