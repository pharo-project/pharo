I'm a Text attribute that tells how content should be aligned.

TextMorph new 
  newContents: (Text streamContents: [:aStream| 
										aStream
 											nextPutAll: 'Left flush' asText; 
											cr;
											nextPutAll: ('Centered' asText addAttribute: TextAlignment centered); 
 											cr;
											nextPutAll: ('Right flush' asText addAttribute: TextAlignment rightFlush); 
 											cr ]);
  openInWindowLabeled: 'TextAlignment demo'