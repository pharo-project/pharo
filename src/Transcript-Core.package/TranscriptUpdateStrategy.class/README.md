TranscriptUpdateStrategy is a superclass for update strategies which can be applied to a transcript. 
After a transcript have updated its outputs with the current content, it will signal the current update strategy (if present) by sending the message #applyUpdateFor: to it. By default this class does no updates :). 
