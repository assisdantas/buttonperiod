
# **Componente TPeriodButton para Lazarus**
### 📋 Descrição
TPeriodButton é um componente personalizado para Lazarus que fornece um botão com menu popup para seleção rápida de períodos de datas pré-definidos. Ideal para formulários que necessitam de filtros por data em aplicações desktop.

### ✨ Funcionalidades

 - 15 Períodos Pré-definidos: Hoje, Ontem, Semana atual, Semana
   anterior, Últimos 15 dias, Mês atual, Mês anterior, Trimestre atual,
   Trimestre anterior, Semestre atual, Semestre anterior, Ano atual, Ano
   anterior
      
  - Separadores Organizacionais: Menu organizado com separadores para
   melhor visualização
   
  - Integração Automática: Conecta-se automaticamente com componentes
   TDateTimePicker
   
  - Eventos Personalizáveis: Dispara eventos quando um período é
   selecionado
   
   Design Flexível: Personalizável através das propriedades padrão do
   TButton

### 🚀 Como Usar
#### Instalação

 1. Compile o pacote `buttonperiod.lpk`
 2. Instale o pacote e recompile o Lazarus
 3. O componente estará disponível na paleta `Misc`do Lazarus

#### Uso Básico

    // No formulário
    procedure TForm1.FormCreate(Sender: TObject);
    begin
      PeriodButton1.StartDatePicker := dtpInicio;
      PeriodButton1.EndDatePicker := dtpFim;
    end;

    // Evento opcional para tratamento
    procedure TForm1.PeriodButton1PeriodSelected(Sender: TObject; 
      PeriodType: TPeriodType; StartDate, EndDate: TDate);
    begin
      ShowMessage('Período selecionado: ' + DateToStr(StartDate) + ' - ' + DateToStr(EndDate));
    end;

### 🎨 Personalização
#### Propriedades Principais

    // Conexão com componentes
    StartDatePicker: TDateTimePicker;  // DateTimePicker para data inicial
    EndDatePicker: TDateTimePicker;    // DateTimePicker para data final
    
    // Acesso às datas
    StartDate: TDate;                  // Data inicial selecionada
    EndDate: TDate;                    // Data final selecionada
    
    // Evento
    OnPeriodSelected: TPeriodSelectedEvent; // Evento ao selecionar período
    Personalização Visual
    
    // Exemplo de personalização
    PeriodButton1.Caption := ' Período';
    PeriodButton1.Width := 120;
    PeriodButton1.Height := 32;
    PeriodButton1.Font.Style := [fsBold];

### 🌟 Vantagens

- **Economia de Tempo:** Elimina a necessidade de codificar manualmente os períodos
- **Consistência:** Garante cálculos consistentes de datas em toda a aplicação
- **Reutilizável:** Um único componente para todos os formulários
- **Manutenção Simplificada:** Atualizações centralizadas em uma única unit

### 📝 Exemplo de Código

// Aplicando um período específico via código
PeriodButton1.ApplyPeriod(ptMesAtual);

// Usando sem componentes visuais
procedure TForm1.PeriodButton1PeriodSelected(Sender: TObject; 
  PeriodType: TPeriodType; StartDate, EndDate: TDate);
begin
  Query1.Params.ParamByName('DATA_INICIO').AsDate := StartDate;
  Query1.Params.ParamByName('DATA_FIM').AsDate := EndDate;
  Query1.Open;
end;

### 🤝 Contribuição

Contribuições são bem-vindas! Sinta-se à vontade para:

- Reportar bugs
- Sugerir novas funcionalidades
- Enviar pull requests
- Melhorar a documentação

### 🔗 Links Úteis
[Lazarus IDE](https://www.lazarus-ide.org/)
[Free Pascal](https://www.freepascal.org/)
[Documentação Lazarus](https://wiki.freepascal.org/Lazarus_Documentation)

⭐ Se este projeto foi útil, deixe uma estrela no repositório!
