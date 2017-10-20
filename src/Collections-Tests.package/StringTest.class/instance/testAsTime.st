tests
testAsTime 

self assert: ('1:00:00' asTime asString) = '1:00 am'.
self assert: ('1:00' asTime asString) = '1:00 am'.
self assert: ('1' asTime asString) = '1:00 am'.
self assert: ('13' asTime asString) = '1:00 pm'.
