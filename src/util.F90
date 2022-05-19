! This file is part of fortty.
! SPDX-Identifier: Apache-2.0
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Utilities for working with platform and compiler specific details
module fortty_util
  implicit none
  private

  public :: is_stdout_a_tty, is_stdin_a_tty


contains

  !> Check output terminal for TTY support
  function is_stdout_a_tty() result(atty)
    use, intrinsic :: iso_fortran_env, only : output_unit
    !> Whether a TTY is found
    logical :: atty
    atty = is_a_tty(output_unit)
  end function is_stdout_a_tty


  !> Check input terminal for TTY support
  function is_stdin_a_tty() result(atty)
    use, intrinsic :: iso_fortran_env, only : input_unit
    !> Whether a TTY is found
    logical :: atty
    atty = is_a_tty(input_unit)
  end function is_stdin_a_tty

  function is_a_tty(unit) result(atty)
#if defined __INTEL_COMPILER
    use ifport, only : isatty
#endif
    !> Unit to check
    integer, intent(in) :: unit
    !> Whether a TTY is found
    logical :: atty

    atty = .false.
#if defined __GFORTRAN__ || defined __INTEL_COMPILER
    atty = isatty(unit)
#endif
  end function is_a_tty
end module fortty_util
