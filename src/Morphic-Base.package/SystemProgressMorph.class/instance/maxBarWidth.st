private
maxBarWidth

	^ self bars inject: 0 into: [ :max :next | next minExtent x max: max ].