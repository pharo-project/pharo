I provide the support to test Pharo Case 13755. Not quite sure how to plug this into the testing framewok.  Currently just run this test manually like this...
    DelaySchedulerBackgroundWorkerMorph new openInWorld

which will FREEZE THE UI when using the following...
  * DelayMillisecondScheduler
  * DelayMicrosecondScheduler

but won't freeze when using  the following...
  * DelayExperimentalSemaphoreScheduler
  * DelayExperimentalSpinScheduler
  * DelayExperimentalCourageousScheduler

Close my instance morphs via halos, or evaluate the following...
    DelaySchedulerBackgroundWorkerMorph allInstances do: #delete.

