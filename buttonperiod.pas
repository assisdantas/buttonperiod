{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit buttonperiod;

{$warn 5023 off : no warning about unused units}
interface

uses
  PeriodButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PeriodButton', @PeriodButton.Register);
end;

initialization
  RegisterPackage('buttonperiod', @Register);
end.
