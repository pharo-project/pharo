column handling
swapColumn: aColumn withColumn: anotherColumn
	"column swapping - this is the default behavior for column drag & drop"
	self swapColumnAt:  aColumn index withColumnAt: anotherColumn index