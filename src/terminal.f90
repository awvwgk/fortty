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

!> Implementation of a terminal to provide ANSI escape sequences
module fortty_terminal
  use fortty_escape, only : ansi_code, escape, operator(+), operator(//), &
    reset, bold, dim, italic, underline, blink, blink_rapid, reverse, hidden, crossed, &
    black, red, green, yellow, blue, magenta, cyan, white, gray, bright_red, &
    bright_green, bright_yellow, bright_blue, bright_magenta, bright_cyan, bright_white, &
    bg_black, bg_red, bg_green, bg_yellow, bg_blue, bg_magenta, bg_cyan, bg_white, &
    bg_gray, bg_bright_red, bg_bright_green, bg_bright_yellow, bg_bright_blue, &
    bg_bright_magenta, bg_bright_cyan, bg_bright_white
  implicit none
  private

  public :: terminal_type
  public :: escape, operator(+), operator(//)


  !> Terminal wrapper to handle color escape sequences, must be initialized with
  !> color support to provide colorful output. Default and uninitialized instances
  !> will remain usable but provide only stubs and do not produce colorful output.
  !> This behavior is useful for creating applications which can toggle color support.
  type :: terminal_type
    type(ansi_code) :: &
      reset = ansi_code(), &
      bold = ansi_code(), &
      dim = ansi_code(), &
      italic = ansi_code(), &
      underline = ansi_code(), &
      blink = ansi_code(), &
      blink_rapid = ansi_code(), &
      reverse = ansi_code(), &
      hidden = ansi_code(), &
      crossed = ansi_code()

    type(ansi_code) :: &
      black = ansi_code(), &
      red = ansi_code(), &
      green = ansi_code(), &
      yellow = ansi_code(), &
      blue = ansi_code(), &
      magenta = ansi_code(), &
      cyan = ansi_code(), &
      white = ansi_code(), &
      gray = ansi_code(), &
      bright_red = ansi_code(), &
      bright_green = ansi_code(), &
      bright_yellow = ansi_code(), &
      bright_blue = ansi_code(), &
      bright_magenta = ansi_code(), &
      bright_cyan = ansi_code(), &
      bright_white = ansi_code()

    type(ansi_code) :: &
      bg_black = ansi_code(), &
      bg_red = ansi_code(), &
      bg_green = ansi_code(), &
      bg_yellow = ansi_code(), &
      bg_blue = ansi_code(), &
      bg_magenta = ansi_code(), &
      bg_cyan = ansi_code(), &
      bg_white = ansi_code(), &
      bg_gray = ansi_code(), &
      bg_bright_red = ansi_code(), &
      bg_bright_green = ansi_code(), &
      bg_bright_yellow = ansi_code(), &
      bg_bright_blue = ansi_code(), &
      bg_bright_magenta = ansi_code(), &
      bg_bright_cyan = ansi_code(), &
      bg_bright_white = ansi_code()
  end type terminal_type

  !> Constructor to create new terminal
  interface terminal_type
    module procedure :: new_terminal
  end interface terminal_type

contains

  !> Create new terminal
  function new_terminal(use_color) result(new)
    !> Enable color support in terminal
    logical, intent(in) :: use_color
    !> New terminal instance
    type(terminal_type) :: new

    if (use_color) then
      new%reset = reset
      new%bold = bold
      new%dim = dim
      new%italic = italic
      new%underline = underline
      new%blink = blink
      new%blink_rapid = blink_rapid
      new%reverse = reverse
      new%hidden = hidden
      new%crossed = crossed

      new%black = black
      new%red = red
      new%green = green
      new%yellow = yellow
      new%blue = blue
      new%magenta = magenta
      new%cyan = cyan
      new%white = white
      new%gray  = gray
      new%bright_red  = bright_red
      new%bright_green  = bright_green
      new%bright_yellow  = bright_yellow
      new%bright_blue  = bright_blue
      new%bright_magenta  = bright_magenta
      new%bright_cyan  = bright_cyan
      new%bright_white  = bright_white

      new%bg_black = bg_black
      new%bg_red = bg_red
      new%bg_green = bg_green
      new%bg_yellow = bg_yellow
      new%bg_blue = bg_blue
      new%bg_magenta = bg_magenta
      new%bg_cyan = bg_cyan
      new%bg_white = bg_white
      new%bg_gray = bg_gray
      new%bg_bright_red = bg_bright_red
      new%bg_bright_green = bg_bright_green
      new%bg_bright_yellow = bg_bright_yellow
      new%bg_bright_blue = bg_bright_blue
      new%bg_bright_magenta = bg_bright_magenta
      new%bg_bright_cyan = bg_bright_cyan
      new%bg_bright_white = bg_bright_white
    end if
  end function new_terminal

end module fortty_terminal
