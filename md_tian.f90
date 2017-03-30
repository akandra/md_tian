program md_tian
    ! Purpose:
    !       Do molecular dynamics, Langevin dynamics, Ring Polymer dynamics
    !
    ! Date          	Author          	    History of Revison
    ! ====          	======          	    ==================
    ! 30.03.2017    	Marvin Kammler		    new data structure
    !                   Sascha Kandratsenka     and propagation mathods
    !
    ! 18.02.2014    	Svenja M. Janke		    Original
    !			        Sascha Kandratsenka
    !			        Dan J. Auerbach
    !


use atom_class

implicit none

type(atoms) :: test

test=atoms(n_beads=2, n_atoms=3)

print*, test



end program md_tian
