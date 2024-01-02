here's what i'm thinking:

daemon to listen for pomo/non-pomo (could actually just be one object--a toggle)
daemon uses dbus
by default non-pomo
fork a thread with forkIO on each call of pomo/non-pomo
this can be achieved with an MVar holding (ThreadId, PomoState)
lib needs to be created for all non-main purposes
