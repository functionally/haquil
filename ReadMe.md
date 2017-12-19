quil: An instruction set for quantum computing
==============================================

This Haskell library implements the Quil language for quantum computing, as specified in "A Practical Quantum Instruction Set Architecture" <<https://arxiv.org/abs/1608.03355/>>, using the indexing conventions in <<https://arxiv.org/abs/1711.02086/>>.

Please report issues at <<https://bwbush.atlassian.net/projects/HQUIL/issues/>>.


Example
-------

This example creates a wavefunction for the [Bell state](https://en.wikipedia.org/wiki/Bell_state) and then performs a measurement on its highest qubit.

	> import Control.Monad.Random (evalRandIO)
	> import Data.Qubit ((^^*), groundState)
	> import Data.Qubit.Gate (H, CNOT)
	
	> -- Construct the Bell state.
        > let bell = [h 0, cnot 0 1] ^^* groundState 2
	> bell
	0.7071067811865475|00> + 0.7071067811865475|11> @ [1,0]
	
	> -- Measure the Bell wavefunction.
	> bell' <- evalRandIO $ measure [1] bell
	> bell'
	([(1,0)],1.0|00> @ [1,0])
	
	> -- Measure it again.
	> evalRandIO $ measure [1] bell'
	([(1,0)],1.0|00> @ [1,0])
	
	> -- Measure another Bell wavefunction.
	> evalRandIO $ measure [1] bell
	([(1,0)],1.0|00> @ [1,0])
	
	> -- Measure another Bell wavefunction.
	> evalRandIO $ measure [1] bell
	([(1,1)],1.0|11> @ [1,0])
