library(Rmpi)

# initialize an Rmpi environment
ns <- as.numeric(commandArgs(trailingOnly=TRUE))
mpi.spawn.Rslaves(nslaves=ns)

# send these commands to the workers
mpi.bcast.cmd( id <- mpi.comm.rank() )
mpi.bcast.cmd( ns <- mpi.comm.size() )
mpi.bcast.cmd( host <- mpi.get.processor.name() )

# all workers execute this command
mpi.remote.exec(paste("I am", id, "of", ns, "running on", host))

# close down the Rmpi environment
mpi.close.Rslaves(dellog = FALSE)
mpi.exit()
