unit PeriodButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, DateTimePicker, DateUtils, Buttons;

type
  TPeriodType = (
    ptHoje,
    ptOntem,
    ptSeparator1,      // Separador
    ptSemanaAtual,
    ptSemanaAnterior,
    ptSeparator2,      // Separador
    ptUltimos15Dias,
    ptMesAtual,
    ptMesAnterior,
    ptSeparator4,      // Separador
    ptTrimestreAtual,
    ptTrimestreAnterior,
    ptSeparator5,      // Separador
    ptSemestreAtual,
    ptSemestreAnterior,
    ptSeparator6,      // Separador
    ptAnoAtual,
    ptAnoAnterior
  );

  TPeriodSelectedEvent = procedure(Sender: TObject; PeriodType: TPeriodType;
    StartDate, EndDate: TDate) of object;

  TDateRange = record
    DataInicio: TDate;
    DataFim: TDate;
  end;

  { TPeriodButton }

  TPeriodButton = class(TBitBtn)
  private
    FPopupMenu: TPopupMenu;
    FStartDatePicker: TDateTimePicker;
    FEndDatePicker: TDateTimePicker;
    FStartDate: TDate;
    FEndDate: TDate;
    FOnPeriodSelected: TPeriodSelectedEvent;
    procedure CreateMenuItems;
    procedure MenuItemClick(Sender: TObject);
    procedure SetStartDatePicker(const Value: TDateTimePicker);
    procedure SetEndDatePicker(const Value: TDateTimePicker);
    procedure UpdateDates;
    function GetDateRange(PeriodType: TPeriodType): TDateRange;
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyPeriod(PeriodType: TPeriodType);
  published
    property StartDatePicker: TDateTimePicker read FStartDatePicker write SetStartDatePicker;
    property EndDatePicker: TDateTimePicker read FEndDatePicker write SetEndDatePicker;
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property OnPeriodSelected: TPeriodSelectedEvent read FOnPeriodSelected write FOnPeriodSelected;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TPeriodButton]);
end;

{ TPeriodButton }

constructor TPeriodButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Period';

  FPopupMenu := TPopupMenu.Create(Self);
  CreateMenuItems;
end;

destructor TPeriodButton.Destroy;
begin
  FPopupMenu.Free;
  inherited Destroy;
end;

procedure TPeriodButton.CreateMenuItems;
const
  MenuItems: array[TPeriodType] of string = (
    'Hoje',
    'Ontem',
    '-',
    'Semana atual',
    'Semana anterior',
    '-',
    'Últimos 15 dias',
    'Mês atual',
    'Mês anterior',
    '-',
    'Trimestre atual',
    'Trimestre anterior',
    '-',
    'Semestre atual',
    'Semestre anterior',
    '-',
    'Ano atual',
    'Ano anterior'
  );
var
  Period: TPeriodType;
  MenuItem: TMenuItem;
begin
  FPopupMenu.Items.Clear;

  for Period := Low(TPeriodType) to High(TPeriodType) do
  begin
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := MenuItems[Period];
    MenuItem.Tag := PtrInt(Period); // No Lazarus usa PtrInt para Tag
    MenuItem.OnClick := @MenuItemClick;
    FPopupMenu.Items.Add(MenuItem);
  end;
end;

procedure TPeriodButton.Click;
var
  P: TPoint;
begin
  inherited Click;
  P := Point(0, Height);
  P := ClientToScreen(P);
  FPopupMenu.Popup(P.X, P.Y);
end;

procedure TPeriodButton.MenuItemClick(Sender: TObject);
var
  PeriodType: TPeriodType;
begin
  PeriodType := TPeriodType(TMenuItem(Sender).Tag);
  ApplyPeriod(PeriodType);
end;

procedure TPeriodButton.ApplyPeriod(PeriodType: TPeriodType);
var
  DateRange: TDateRange;
begin
  DateRange := GetDateRange(PeriodType);
  FStartDate := DateRange.DataInicio;
  FEndDate := DateRange.DataFim;

  UpdateDates;

  if Assigned(FOnPeriodSelected) then
    FOnPeriodSelected(Self, PeriodType, FStartDate, FEndDate);
end;

function TPeriodButton.GetDateRange(PeriodType: TPeriodType): TDateRange;
var
  DataRef: TDate;
  Mes, Trimestre, TrimestreAnterior, Semestre, SemestreAnterior, Ano: Word;
begin
  DataRef := Date;

  case PeriodType of
    ptHoje:
      begin
        Result.DataInicio := DataRef;
        Result.DataFim := DataRef;
      end;
    ptOntem:
      begin
        Result.DataInicio := IncDay(DataRef, -1);
        Result.DataFim := IncDay(DataRef, -1);
      end;
    ptSemanaAtual:
      begin
        Result.DataInicio := StartOfTheWeek(DataRef);
        Result.DataFim := EndOfTheWeek(DataRef);
      end;
    ptSemanaAnterior:
      begin
        DataRef := IncWeek(DataRef, -1);
        Result.DataInicio := StartOfTheWeek(DataRef);
        Result.DataFim := EndOfTheWeek(DataRef);
      end;
    ptUltimos15Dias:
      begin
        Result.DataInicio := IncDay(DataRef, -14);
        Result.DataFim := DataRef;
      end;
    ptMesAtual:
      begin
        Result.DataInicio := StartOfTheMonth(DataRef);
        Result.DataFim := EndOfTheMonth(DataRef);
      end;
    ptMesAnterior:
      begin
        DataRef := IncMonth(DataRef, -1);
        Result.DataInicio := StartOfTheMonth(DataRef);
        Result.DataFim := EndOfTheMonth(DataRef);
      end;
    ptTrimestreAtual:
      begin
        Mes := MonthOf(DataRef);
        Trimestre := (Mes - 1) div 3 + 1;
        Result.DataInicio := EncodeDate(YearOf(DataRef), (Trimestre - 1) * 3 + 1, 1);
        Result.DataFim := EndOfTheMonth(EncodeDate(YearOf(DataRef), (Trimestre - 1) * 3 + 3, 1));
      end;
    ptTrimestreAnterior:
      begin
        Mes := MonthOf(DataRef);
        TrimestreAnterior := (Mes - 1) div 3;
        Ano := YearOf(DataRef);
        if TrimestreAnterior = 0 then
        begin
          TrimestreAnterior := 4;
          Dec(Ano);
        end;
        Result.DataInicio := EncodeDate(Ano, (TrimestreAnterior - 1) * 3 + 1, 1);
        Result.DataFim := EndOfTheMonth(EncodeDate(Ano, (TrimestreAnterior - 1) * 3 + 3, 1));
      end;
    ptSemestreAtual:
      begin
        Mes := MonthOf(DataRef);
        Semestre := (Mes - 1) div 6 + 1;
        Result.DataInicio := EncodeDate(YearOf(DataRef), (Semestre - 1) * 6 + 1, 1);
        Result.DataFim := EndOfTheMonth(EncodeDate(YearOf(DataRef), (Semestre - 1) * 6 + 6, 1));
      end;
    ptSemestreAnterior:
      begin
        Mes := MonthOf(DataRef);
        SemestreAnterior := (Mes - 1) div 6;
        Ano := YearOf(DataRef);
        if SemestreAnterior = 0 then
        begin
          SemestreAnterior := 2;
          Dec(Ano);
        end;
        Result.DataInicio := EncodeDate(Ano, (SemestreAnterior - 1) * 6 + 1, 1);
        Result.DataFim := EndOfTheMonth(EncodeDate(Ano, (SemestreAnterior - 1) * 6 + 6, 1));
      end;
    ptAnoAtual:
      begin
        Result.DataInicio := EncodeDate(YearOf(DataRef), 1, 1);
        Result.DataFim := EncodeDate(YearOf(DataRef), 12, 31);
      end;
    ptAnoAnterior:
      begin
        Ano := YearOf(DataRef) - 1;
        Result.DataInicio := EncodeDate(Ano, 1, 1);
        Result.DataFim := EncodeDate(Ano, 12, 31);
      end;
  else
    Result.DataInicio := DataRef;
    Result.DataFim := DataRef;
  end;
end;

procedure TPeriodButton.UpdateDates;
begin
  if Assigned(FStartDatePicker) then
    FStartDatePicker.Date := FStartDate;

  if Assigned(FEndDatePicker) then
    FEndDatePicker.Date := FEndDate;
end;

procedure TPeriodButton.SetStartDatePicker(const Value: TDateTimePicker);
begin
  FStartDatePicker := Value;
  if Assigned(FStartDatePicker) then
    FStartDate := FStartDatePicker.Date;
end;

procedure TPeriodButton.SetEndDatePicker(const Value: TDateTimePicker);
begin
  FEndDatePicker := Value;
  if Assigned(FEndDatePicker) then
    FEndDate := FEndDatePicker.Date;
end;

end.
