module run_config
    !
    ! Purpose:
    !    Initialize simulation parameters
    !
    ! Date          	Author          	History of Revison
    ! ====          	======          	==================
    ! 31.03.2017    	Marvin Kammler		    original
    !                   Sascha Kandratsenka


implicit none

contains

subroutine read_input_file(input_file)

character(*), intent(in) :: input_file

integer :: ios = 0, line = 0, pos1
character(len=200) :: buffer, label!, mdpa_name_p,mdpa_name_l, biffer


!------------------------------------------------------------------------------
!                       READ IN INPUT FILE
!                       ==================
!------------------------------------------------------------------------------

print *, input_file
    call open_for_read(38, input_file)
    ! ios < 0: end of record condition encountered or endfile condition detected
    ! ios > 0: an error is detected
    ! ios = 0  otherwise

    do while (ios == 0)
        read(38, '(A)', iostat=ios) buffer
        if (ios == 0) then
            line = line + 1
            ! Find the first instance of whitespace.  Split label and data.
            pos1 = scan(buffer, ' ')
            label = buffer(1:pos1)
            buffer = buffer(pos1+1:)
print *, buffer, label
            select case (label)
!            case('start')
!                read(buffer,*,iostat=ios) start_tr
!            case('ntrajs')
!                read(buffer,*,iostat=ios) ntrajs
!            case('Einc')
!                read(buffer,*,iostat=ios) einc
!            case('inclination')
!                read(buffer,*,iostat=ios) inclination
!                inclination = inclination*deg2rad
!            case('azimuth')
!                read(buffer,*,iostat=ios) azimuth
!                azimuth = azimuth*deg2rad
!            case('Tsurf')
!                read(buffer,*,iostat=ios) Tsurf
!                Tsurf_key = .true.
!            case('step')
!                read(buffer,*,iostat=ios) step
!            case('nsteps')
!                read(buffer,*,iostat=ios) nsteps
!            case('wstep')
!                read(buffer,*,iostat=ios) wstep
!                if (wstep(1)==-1) wstep(2) = nsteps + 1
!            case ('projectile')
!                read(buffer, *, iostat=ios) name_p, mass_p, npars_p, &
!                                            key_p, mdpa_name_p, n_p0
!
!                md_algo_p_key = .true.
!                mass_p=mass_p*amu2mass
!                call lower_case(mdpa_name_p)
!                select case (mdpa_name_p(1:3))
!                    case ('ver')
!                        md_algo_p = 1
!                    case ('bee')
!                        md_algo_p = 2
!                    case ('lan')
!                        md_algo_p = 3
!                    case ('sla')
!                        md_algo_p = 4
!                    case ( 'pef' )
!                        md_algo_p = 5
!                   case default
!                        print *, 'algorithm ', trim(mdpa_name_p), ' unknown'
!                        stop
!                end select
!                allocate(pars_p(npars_p))
!                call open_for_read(23,key_p)
!                read(23,'(/)')
!                do i = 1, npars_p
!                    read(23,*) biffer, pars_p(i)
!                end do
!                close(23)
!
!            case ('lattice')
!                read(buffer, *, iostat=ios) name_l, mass_l, npars_l, &
!                                            key_l, mdpa_name_l, nlno
!                md_algo_l_key = .true.
!                mass_l=mass_l*amu2mass
!                call lower_case(mdpa_name_l)
!                select case (mdpa_name_l(1:3))
!                    case ('ver')
!                        md_algo_l = 1
!                    case ('bee')
!                        md_algo_l = 2
!                    case ('lan')
!                        md_algo_l = 3
!                    case ('sla')
!                        md_algo_l = 4
!                    case ( 'pef' )
!                        md_algo_l = 5
!                   case default
!                        print *, 'algorithm ', trim(mdpa_name_l), ' unknown'
!                        stop
!                end select
!                allocate(pars_l(npars_l))
!                call open_for_read(23,key_l)
!                read(23,'(/)')
!                do i = 1, npars_l
!                    read(23,*) biffer, pars_l(i)
!                end do
!                close(23)
!            case ('celldim')
!                read(buffer, *, iostat=ios) celldim, celln
!                call lower_case(celln)
!                if (celln == 'atlayer') then
!                    allocate(nr_at_layer(celldim(3)))
!                    read(buffer,*,iostat=ios) celldim, celln, nr_at_layer ! gives number of atoms in layer if not calculatable from celldim
!                end if
!            case ('pip')
!                read(buffer, *, iostat=ios) pip_sign
!                pos1 = scan(buffer, ' ')
!                label = buffer(1:pos1)
!                buffer = buffer(pos1+1:)
!                select case(pip_sign)
!                    case(0)
!                        read(buffer, *, iostat=ios) height
!                        if (ios .ne. 0) then
!                            print *, 'Warning: You have not specified a &
!                                               projectile height.'
!                            print *, '         Projectile height set to 6.0 A.'
!                        end if
!
!                    case(1)
!                        read(buffer, *, iostat=ios) key_p_pos, height
!                        if (ios .ne. 0) then
!                            read(buffer, *, iostat=ios) key_p_pos
!                            if (ios == 0) then
!                                print *, 'Warning: You have not specified a &
!                                          projectile height.'
!                                print *, '         Height set to 6.0 A.'
!                            else
!                                print *, 'Warning: You have not specified a &
!                                                   projectile position.'
!                                print *, '         Projectile position assigned to top &
!                                                at 6.0 A'
!                            end if
!                        end if
!
!                    case(2)
!                        if (ios == 0) then
!                            read(buffer, *, iostat=ios) key_p_pos
!                        else
!                            print *, 'Warning: You have not specified a &
!                                               projectile position.'
!                            print *, '         Projectile position assigned to top &
!                                                at 6.0 A.'
!                            pip_sign = 1
!                        end if
!                    case(3) ! Place the atom really at nice, random position on or in surface
!                            ! and set velocities to slab temperature
!                        read(buffer, *, iostat=ios) key_p_pos, height
!                        if (ios .ne. 0) then
!                            read(buffer, *, iostat=ios) key_p_pos
!                            if (ios == 0) then
!                                print *, 'Warning: You have not specified a &
!                                          projectile height.'
!                                print *, '         Height set to 6.0 A.'
!                            else
!                                print *, 'Warning: You have not specified a &
!                                                   projectile position.'
!                                print *, '         Projectile position assigned to top &
!                                                at 6.0 A'
!                            end if
!                        end if
!                    case(-1)
!                    case default
!                        print '(A,I4,A)', 'Warning: Pip Flag value ', pip_sign, ' does not match any possible options.'
!                        print *, '            Setting Pip to top at 6.0 A'
!                        pip_sign = 1
!                end select
!
!            case ('pes')
!                read(buffer, *, iostat=ios) pes_name!, pes_nigh
!                call lower_case(pes_name)
!                select case(pes_name)
!                    case('emt')
!                        pes_key = 0
!                    case('lj')
!                        pes_key = 1
!                    case default
!                        print *, 'Error: Unknown PES.'
!                        stop ' subroutine: simbox_init()'
!                end select
!
!            case ('rep')
!
!                read(buffer, *, iostat=ios) rep
!                if (ios /= 0) then
!
!                    read(buffer, *, iostat=ios) rep(1)
!                    if (ios == 0) then
!                        rep(2) = rep(1)
!                        print *, 'Warning: You have specified a single number for cell &
!                                           repetitions.'
!                        print *, '         Using the same repetition in both directions.'
!                    else
!                        print *, 'Warning: You have not specified number of cell repetitions.'
!                        print *, '         Set to 2x2.'
!                   end if
!
!                end if
!
!            case ('conf')
!                read(buffer, *, iostat=ios) confname, confname_file
!                if (confname == 'geo') read(buffer, *, iostat=ios) &
!                                       confname, confname_file, conf_nr
!                if (confname == 'mxt') read(buffer, *, iostat=ios) &
!                                       confname, confname_file, n_confs
!                if (confname == 'fit') read(buffer, *, iostat=ios) &
!                                       confname, confname_file, n_confs, fitnum
!                call lower_case(confname)
!            case ('evasp')
!                read(buffer, *, iostat=ios) evasp
!            case ('fitconst')
!                read(buffer, *, iostat=ios) ipc, (ibt(i), i=1,ipc)
!            case ('maxit')
!                read(buffer, *, iostat=ios) max_iterations
!            case('anneal')
!                read(buffer, *, iostat=ios) Tmax, sasteps
!             case('trajname')
!                read(buffer,*,iostat=ios) trajn
!                pos1 = scan(buffer, ' ')
!                buffer = buffer(pos1+1:)
!                allocate(trajname(trajn))
!                read(buffer, *, iostat=ios) trajname
!            case('fitmix')
!                read(buffer,*,iostat=ios) fracaimd
!            case default
!                print *, 'Skipping invalid label at line', line, label
!            case('3Dgrid')
!                read(buffer,*,iostat=ios) dftlu, nsites
!                allocate(ssites(nsites))
!                pos1 = scan(buffer, ' ')
!                buffer = buffer(pos1+1:)
!                pos1 = scan(buffer, ' ')
!                buffer = buffer(pos1+1:)
!                pos1 = scan(buffer, ' ')
!                buffer = buffer(pos1+1:)
!                read(buffer, *, iostat=ios) ssites
!            case ('aimd')
!                read(buffer,*,iostat=ios) aimdlu, distmima
            end select
        end if
    end do ! ios

end subroutine read_input_file

end module run_config
