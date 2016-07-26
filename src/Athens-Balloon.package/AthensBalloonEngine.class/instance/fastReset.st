private
fastReset

	workBuffer ifNil: [ ^ self reset ].

	workBuffer 
		at: (1+GWState) put: GEStateUnlocked;
		at: (1+GWObjUsed) put: 4;
		at: (1+GWNeedsFlush) put: 0;
		at: (1+GWCurrentZ) put: 0;
		at: (1+GWGETStart) put: 0";

		at: (1+GWGETUsed) put: 0;
		at: (1+GWAETStart) put: 0;
		at: (1+GWAETUsed) put: 0".
	
	self primSetEdgeTransform: edgeTransform.
	self primSetClipRect: clipRect.
	self primSetColorTransform: colorTransform.
	self primSetDepth: self primGetDepth + 1. 

	postFlushNeeded := false.
	forms := #().

"	
-	self magicNumberPut: GWMagicNumber.
- 	self wbSizePut: size.
+	self wbTopPut: size.
+	self statePut: GEStateUnlocked.
-	self objStartPut: GWHeaderSize.
+	self objUsedPut: 4.	
-	self objectTypeOf: 0 put: GEPrimitiveFill.
-	self objectLengthOf: 0 put: 4.
-	self objectIndexOf: 0 put: 0.

	self getStartPut: 0.
	self getUsedPut: 0.
	self aetStartPut: 0.
	self aetUsedPut: 0.
	self stopReasonPut: 0.

+	self needsFlushPut: 0.
-	self clipMinXPut: 0.
-	self clipMaxXPut: 0.
-	self clipMinYPut: 0.
-	self clipMaxYPut: 0.
+	self currentZPut: 0.
	self resetGraphicsEngineStats.
	self initEdgeTransform.
	self initColorTransform.
"